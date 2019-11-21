{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Generics.ApplyTwins
  ( ApplyTwins (), apTwins
  , ApplyTwinsDef (), GMap (), apTwinsDef
  ) where


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


-- | Combine two arbitrary generic functor-like objects
--    that have almost the same constructor structure.
--
--   If the first constructor does not match the second one,
--    default function is used on the second.
apTwinsDef :: ( Generic (m (x->y)), Generic (m x), Generic (m y)
              , ApplyTwinsDef (Rep (m (x->y))) (Rep (m x)) (Rep (m y)) x y
              )
           => (x -> y) -> m (x -> y) -> m x -> m y
apTwinsDef y a b = to $ aptWithDef y (from a) (from b)


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



class GMap mx my x y where
  gmap :: (x -> y) -> mx p -> my p

class ApplyTwinsDef mxy mx my x y where
  aptWithDef :: (x -> y) -> mxy p -> mx p -> my p

instance GMap U1 U1 x y where
  gmap _ U1 = U1

instance {-# OVERLAPPING #-}
         GMap (K1 i x) (K1 i y) x y where
  gmap f (K1 x) = K1 (f x)

instance GMap (K1 i a) (K1 i a) x y where
  gmap _ (K1 x) = K1 x

instance {-# OVERLAPPABLE #-}
         ( Generic fx, Generic fy
         , GMap (Rep fx) (Rep fy) x y
         )
      => GMap (K1 i fx) (K1 i fy) x y where
  gmap f (K1 fx) = K1 . to $ gmap f (from fx)

instance GMap fx fy x y
      => GMap (M1 i c fx) (M1 i c fy) x y where
  gmap f (M1 fx) = M1 $ gmap f fx

instance GMap fx fy x y
      => GMap (Rec1 fx) (Rec1 fy) x y where
  gmap f  (Rec1 fx) = Rec1 $ gmap f fx

instance ( GMap fx fy x y
         , GMap gx gy x y
         )
      => GMap (fx :+: gx) (fy :+: gy) x y where
  gmap f (L1 fx) = L1 $ gmap f fx
  gmap f (R1 gx) = R1 $ gmap f gx

instance ( GMap fx fy x y
         , GMap gx gy x y
         )
      => GMap (fx :*: gx) (fy :*: gy) x y where
  gmap f (fx :*: gx) = gmap f fx :*: gmap f gx

instance ( Functor f
         , GMap gx gy x y
         )
      => GMap (f :.: gx) (f :.: gy) x y where
  gmap f (Comp1 fgx) = Comp1 $ gmap f <$> fgx



instance ApplyTwinsDef U1 U1 U1 x y where
  aptWithDef _ U1 U1 = U1

instance ApplyTwinsDef (K1 i a) (K1 i a) (K1 i a) x y where
  aptWithDef _ (K1 _) (K1 x) = K1 x

instance {-# OVERLAPPING #-}
         ApplyTwinsDef (K1 i (x->y)) (K1 i x) (K1 i y) x y where
  aptWithDef _ (K1 f) (K1 x) = K1 (f x)

instance {-# OVERLAPPABLE #-}
         ( Generic fxy, Generic fx, Generic fy
         , ApplyTwinsDef (Rep fxy) (Rep fx) (Rep fy) x y
         )
      => ApplyTwinsDef (K1 i fxy) (K1 i fx) (K1 i fy) x y where
  aptWithDef y (K1 ff) (K1 fx) = K1 . to $ aptWithDef y (from ff) (from fx)

instance ApplyTwinsDef fxy fx fy x y
      => ApplyTwinsDef (M1 i c fxy) (M1 i c fx) (M1 i c fy) x y where
  aptWithDef y (M1 ff) (M1 fx) = M1 $ aptWithDef y ff fx

instance ApplyTwinsDef fxy fx fy x y
      => ApplyTwinsDef (Rec1 fxy) (Rec1 fx) (Rec1 fy) x y where
  aptWithDef y (Rec1 ff) (Rec1 fx) = Rec1 $ aptWithDef y ff fx

instance ( ApplyTwinsDef fxy fx fy x y
         , ApplyTwinsDef gxy gx gy x y
         , GMap fx fy x y
         , GMap gx gy x y
         )
      => ApplyTwinsDef (fxy :+: gxy) (fx :+: gx) (fy :+: gy) x y where
  aptWithDef f (L1 ff) (L1 fx) = L1 $ aptWithDef f ff fx
  aptWithDef f (R1 gf) (R1 gx) = R1 $ aptWithDef f gf gx
  aptWithDef f  _      (L1 fx) = L1 $ gmap f fx
  aptWithDef f  _      (R1 gx) = R1 $ gmap f gx

instance ( ApplyTwinsDef fxy fx fy x y
         , ApplyTwinsDef gxy gx gy x y
         )
      => ApplyTwinsDef (fxy :*: gxy) (fx :*: gx) (fy :*: gy) x y where
  aptWithDef y (ff :*: gf) (fx :*: gx)
    = aptWithDef y ff fx :*: aptWithDef y gf gx

instance ( Applicative f
         , ApplyTwinsDef gxy gx gy x y
         )
      => ApplyTwinsDef (f :.: gxy) (f :.: gx) (f :.: gy) x y where
  aptWithDef y (Comp1 fgf) (Comp1 fgx) = Comp1 $ aptWithDef y <$> fgf <*> fgx
