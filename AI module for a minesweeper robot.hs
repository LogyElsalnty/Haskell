type Cell = (Int,Int)
data MyState = Null | S Cell [Cell] String MyState deriving (Show,Eq)

getX (S (x,y) l _ _)=x
getY (S (x,y) l _ _)=y
getList (S (x,y) l _ _)=l

tempX (x,y)= x
tempY (x,y)= y
isMemberOf _ []=False
isMemberOf (getX,getY) (h:t)=if(getX == (tempX h) && getY == (tempY h))
							 then True
							 else isMemberOf (getX,getY) t
removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys


up:: MyState -> MyState 
up s=   if((getX s)>0)
		then (S (pointUp s) (getList s) "up" (s))
		else Null

pointUp (S (x,y) l _ _)=((x-1),y)

down:: MyState -> MyState 
down s= if((getX s)<3)
		then (S (pointDown s) (getList s) "down" (s))
		else Null

pointDown (S (x,y) l _ _)=((x+1),y)

right:: MyState -> MyState
right s= if((getY s)<3)
		then (S (pointRight s) (getList s) "right" (s))
		else Null

pointRight (S (x,y) l _ _)=(x,(y+1))

left:: MyState -> MyState
left s= if((getY s)>0)
		then (S (pointLeft s) (getList s) "left" (s))
		else Null

pointLeft (S (x,y) l _ _)=(x,(y-1))

collect:: MyState -> MyState
collect s= if(isMemberOf (getX s,getY s) (getList s))
		   then (S (getX s, getY s) (removeItem (getX s, getY s) (getList s)) "collect" (s))
		   else Null
		   
nextMyStates::MyState->[MyState]
nextMyStates s= gen s []
gen s l=if(collect s/=Null)
		then l ++ [collect s] ++ gen1 s l
		else l ++ gen1 s l
gen1 s l=if(up s/=Null)
		then l ++ [up s] ++ gen2 s l
		else l ++ gen2 s l
gen2 s l=if(down s/=Null)
		then l ++ [down s] ++ gen3 s l
		else l ++ gen3 s l
gen3 s l=if(left s/=Null)
		then l ++ [left s] ++ gen4 s l
		else l ++ gen4 s l
gen4 s l=if(right s/=Null)
		then l ++ [right s]
		else l


isGoal :: MyState -> Bool
isGoal s= if(getList s == [])
		  then True
		  else False
		  
search::[MyState]->MyState
search (h:t) = if(isGoal h)
			   then h
			   else search (t ++ nextMyStates h)
		   
constructSolution:: MyState ->[String]
constructSolutionH (S _ _ s Null)= s:[]
constructSolutionH (S _ _ s q)= removeItem "" ([s] ++ constructSolutionH q)
constructSolution s=reverse (constructSolutionH s)

solve :: Cell->[Cell]->[String]
solve (x,y) l=  if(x>3 || y>3 || x<0 || y<0)
				then error "Out of grid"
				else constructSolution(search [(S (x,y) l "" Null)])