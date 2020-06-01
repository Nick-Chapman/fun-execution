
keep = fix \_ x. x -- prevent Nbe from inlining

-- example which causes over-application
-- and needs to have the "ap" set correctly

func a = let b = a+a in \c. b*c

prog n = keep func n n

prog 10 -- expect 200
