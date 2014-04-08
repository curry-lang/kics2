-- Examples for testing library SetFunctions
--
-- To run all tests automatically by the currytest tool, use the command:
-- "currytest testSetFunctions"

import Assertion
import SetFunctions
import Integer(abs)

-------------------------------------------------------------------------
-- Simple classical tests for set functions

coin = 0 ? 1

bigCoin = 2 ? 4

f x = coin + x

test1 = assertValues "NoSetFun" (f bigCoin) [2,3,4,5]

test2 = assertValues "SetFun_f_bigCoin"
                     (foldValues (+) 0 (set1 f bigCoin))
                     [5,9]

-------------------------------------------------------------------------
-- The flight example from the paper introducing set functions

data FlightNumber = LH469 | NWA92 | LH10 | KL1783

data City = Portland | Frankfurt | Amsterdam | Hamburg

x :. y = (x,y)

flight = (LH469, Portland, Frankfurt,10:.15)
flight = (NWA92, Portland, Amsterdam,10:.00)
flight = (LH10,  Frankfurt,Hamburg,   1:.00)
flight = (KL1783,Amsterdam,Hamburg,   1:.52)

itinerary orig dest 
   | flight =:= (num,orig,dest,len)
   = [num]
   where num, len free
itinerary orig dest 
   | flight =:= (num1,orig,x,len1)
   & flight =:= (num2,x,dest,len2)
   = [num1,num2]
   where num1, len1, num2, len2, x free

duration :: [FlightNumber] -> Int
duration = foldr (+) 0 . map flightToMinutes

flightToMinutes :: FlightNumber -> Int
flightToMinutes fnum | flight =:= (fnum,unknown,unknown,h:.m)
                     = h*60+m
  where h,m free

-- Returns an itinerary with a shortest flight time.
-- Purely declarative specification: an itinerary is the shortest if there is
-- no shorter itinerary.
shortestItin s e 
   | isEmpty (set1 shorterItinThan (duration it))
   = it
   where it = itinerary s e
         shorterItinThan itduration
            | duration its < itduration
            = its
            where its = itinerary s e

testFlight1 = assertEqual "Flight1" (shortestItin Portland Hamburg) [LH469,LH10]

-- Returns an itinerary with a shortest flight time.
-- Implemented by selecting the shortest path from all pathes.
shortestItinSelect s e 
   = minValue shorter (set2 itinerary s e)
   where shorter it1 it2 
            = duration it1 <= duration it2

testFlight2 = assertEqual "Flight2" (shortestItinSelect Portland Hamburg)
                                    [LH469,LH10]


-------------------------------------------------------------------------
-- Definition of n-queens with set functions

perm [] = []
perm (x:xs) = ndinsert x (perm xs)
 where
  ndinsert x ys     = x : ys
  ndinsert x (y:ys) = y : ndinsert x ys


queens n | isEmpty ((set1 unsafe) p) = p
 where p = perm [1..n]

       unsafe (_++[x]++y++[z]++_) = abs (x-z) =:= length y + 1


testq1 = assertValues "Queens4" (queens 4) [[3,1,4,2],[2,4,1,3]]

testq2 = assertValues "Queens6" (queens 6)
             [[5,3,1,6,4,2],[4,1,5,2,6,3],[3,6,2,5,1,4],[2,4,6,1,3,5]]

-------------------------------------------------------------------------
-- Set functions with infinite arguments (does not work with PAKCS)

coinList = coin : coinList

testInf1 = assertValues "Infinite argument"
                        (foldValues (+) 0 (set1 head coinList))
                        [0,1]

nums x = x : nums (x+1)

anyElem (x:xs) = x ? anyElem xs

testInf2 = assertEqual "Infinite set and argument"
                       (isEmpty (set1 anyElem (nums 0)))
                       False

-------------------------------------------------------------------------
-- Set functions that use higher order functions to smuggle
-- non-determinism from outside into a set-function
-- yields wrong results with the initial implementation


applyUnit f = f ()

testSmuggle = assertValues "Smuggle non-determinism"
                   (let x = True ? False in
                      (sortValues (set1 applyUnit (\_ -> x))))
		   [[True],[False]]


-------------------------------------------------------------------------
-- Set function that uses information from outside to determine
-- the result of the set function
-- Shows that narrowing of free variables from outside is necessary


bindTo x = x =:= y &> y
 where y free

testBindToBound = assertEqual "Bind to bound"
                    (x =:= True &> set1 bindTo x)
                    (set0 True)
 where x free
