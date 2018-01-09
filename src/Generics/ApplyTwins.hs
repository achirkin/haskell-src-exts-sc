{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
module Generics.ApplyTwins (ApplyTwins (), apTwins) where


import           GHC.Generics

-- | Combine two arbitrary generic functor-like objects
--    that have exactly same constructor structure (w.r.t. the type parameter).
--
--   Being applied to two Const functors, prefers value of the first one.
apTwins :: ( Generic (m (x->y)), Generic (m x), Generic (m y)
           , ApplyTwins (Rep (m (x->y))) (Rep (m x)) (Rep (m y))
           )
         => m (x -> y) -> m x -> Maybe (m y)
apTwins a b = to <$> apt (from a) (from b)


class ApplyTwins mxy mx my where
  apt :: mxy p -> mx p -> Maybe (my p)

instance ApplyTwins U1 U1 U1 where
  apt U1 U1 = Just U1

instance ApplyTwins (K1 i x) (K1 i x) (K1 i x) where
  apt (K1 x) (K1 _) = Just (K1 x)

instance {-# OVERLAPPING #-}
         ApplyTwins (K1 i (x->y)) (K1 i x) (K1 i y) where
  apt (K1 f) (K1 x) = Just (K1 (f x))

instance {-# OVERLAPPABLE #-}
         ( Generic fxy, Generic fx, Generic fy
         , ApplyTwins (Rep fxy) (Rep fx) (Rep fy)
         )
      => ApplyTwins (K1 i fxy) (K1 i fx) (K1 i fy) where
  apt (K1 ff) (K1 fx) = K1 . to <$> apt (from ff) (from fx)

instance ApplyTwins fxy fx fy
      => ApplyTwins (M1 i c fxy) (M1 i c fx) (M1 i c fy) where
  apt (M1 ff) (M1 fx) = M1 <$> apt ff fx

instance ApplyTwins fxy fx fy
      => ApplyTwins (Rec1 fxy) (Rec1 fx) (Rec1 fy) where
  apt (Rec1 ff) (Rec1 fx) = Rec1 <$> apt ff fx

instance ( ApplyTwins fxy fx fy
         , ApplyTwins gxy gx gy
         )
      => ApplyTwins (fxy :+: gxy) (fx :+: gx) (fy :+: gy) where
  apt (L1 ff) (L1 fx) = L1 <$> apt ff fx
  apt (R1 gf) (R1 gx) = R1 <$> apt gf gx
  apt  _       _      = Nothing

instance ( ApplyTwins fxy fx fy
         , ApplyTwins gxy gx gy
         )
      => ApplyTwins (fxy :*: gxy) (fx :*: gx) (fy :*: gy) where
  apt (ff :*: gf) (fx :*: gx) = (:*:) <$> apt ff fx <*> apt gf gx

instance ( Applicative f, Traversable f
         , ApplyTwins gxy gx gy
         )
      => ApplyTwins (f :.: gxy) (f :.: gx) (f :.: gy) where
  apt (Comp1 fgf) (Comp1 fgx) = fmap Comp1. sequence $ apt <$> fgf <*> fgx
