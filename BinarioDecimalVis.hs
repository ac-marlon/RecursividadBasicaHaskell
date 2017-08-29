decbin::Int->Int
decbin 0 = 0
decbin 1 = 1
decbin a = decbin(a mod 2) * 10
