module Vec3
    (Vec3(..)
    ,(.+.)
    ,(.-.)
	,(.*.)
    ,dot
    ,vec3fromList
    ,vec3toList
    ) where

data Vec3 a = Vec3 !a !a !a deriving(Eq)
instance (Show a) => Show (Vec3 a) where
    show (Vec3 x y z) = "{" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ "}"

instance Functor Vec3 where
    fmap f (Vec3 x y z) = Vec3 (f x) (f y) (f z)

vec3fromList :: (Num a) => [a] -> Vec3 a
vec3fromList [x, y, z] = Vec3 x y z
vec3fromList _         = Vec3 0 0 0

vec3toList :: (Num a) => Vec3 a -> [a]
vec3toList (Vec3 x y z) = [x, y, z]

(.+.) :: (Num a) => Vec3 a -> Vec3 a -> Vec3 a
(Vec3 x1 y1 z1) .+. (Vec3 x2 y2 z2)  = Vec3 (x1+x2) (y1+y2) (z1+z2)

(.-.) :: (Num a) => Vec3 a -> Vec3 a -> Vec3 a
(Vec3 x1 y1 z1) .-. (Vec3 x2 y2 z2)  = Vec3 (x1-x2) (y1-y2) (z1-z2)

(.*.) :: (Num a) => Vec3 a -> Vec3 a -> Vec3 a
(Vec3 x1 y1 z1) .*. (Vec3 x2 y2 z2)  = Vec3 (x1*x2) (y1*y2) (z1*z2)

dot :: (Num a) => Vec3 a -> Vec3 a -> a
(Vec3 x1 y1 z1) `dot` (Vec3 x2 y2 z2) = x1*x2 + y1*y2 + z1*z2
