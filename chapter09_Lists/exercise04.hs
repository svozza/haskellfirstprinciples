mySqr  = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

myTuples = [(x, y) | x <- mySqr, y <- myCube]
myTuplesLessThan50 = [(x, y) | (x, y) <- myTuples, x < 50, y < 50]

amountOfTuplesLessThan50 = length myTuplesLessThan50
