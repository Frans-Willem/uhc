%%[99
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -XNoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}
{-# EXCLUDE_IF_TARGET jscript #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Stable
-- Copyright   :  (c) The University of Glasgow, 1992-2004
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  ffi@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Stable pointers.
--
-- Adapted for use in EHC
--
-----------------------------------------------------------------------------

-- #hide
module UHC.StablePtr
        ( StablePtr(..)
        , newStablePtr          -- :: a -> IO (StablePtr a)    
        , deRefStablePtr        -- :: StablePtr a -> a
        , freeStablePtr         -- :: StablePtr a -> IO ()
        , castStablePtrToPtr    -- :: StablePtr a -> Ptr ()
        , castPtrToStablePtr    -- :: Ptr () -> StablePtr a
   ) where

import UHC.Ptr
import UHC.Base
-- import UHC.IOBase

-----------------------------------------------------------------------------
-- Stable Addr

foreign import prim primMakeStableAddr  :: a -> Addr
foreign import prim primDeRefStableAddr :: Addr -> a
foreign import prim primFreeStableAddr  :: Addr -> ()
foreign import prim primEqStableAddr    :: Addr -> Addr -> Bool

-----------------------------------------------------------------------------
-- Stable Pointers

{- |
A /stable pointer/ is a reference to a Haskell expression that is
guaranteed not to be affected by garbage collection, i.e., it will neither be
deallocated nor will the value of the stable pointer itself change during
garbage collection (ordinary references may be relocated during garbage
collection).  Consequently, stable pointers can be passed to foreign code,
which can treat it as an opaque reference to a Haskell value.

A value of type @StablePtr a@ is a stable pointer to a Haskell
expression of type @a@.
-}
newtype StablePtr a = StablePtr Addr

-- |
-- Create a stable pointer referring to the given Haskell value.
--
newStablePtr   :: a -> IO (StablePtr a)
newStablePtr a = ioFromPrim (\_ -> StablePtr (primMakeStableAddr a))

-- |
-- Obtain the Haskell value referenced by a stable pointer, i.e., the
-- same value that was passed to the corresponding call to
-- 'makeStablePtr'.  If the argument to 'deRefStablePtr' has
-- already been freed using 'freeStablePtr', the behaviour of
-- 'deRefStablePtr' is undefined.
--
deRefStablePtr :: StablePtr a -> IO a
deRefStablePtr (StablePtr sp) = ioFromPrim (\_ -> primDeRefStableAddr sp)

-- |
-- Dissolve the association between the stable pointer and the Haskell
-- value. Afterwards, if the stable pointer is passed to
-- 'deRefStablePtr' or 'freeStablePtr', the behaviour is
-- undefined.  However, the stable pointer may still be passed to
-- 'castStablePtrToPtr', but the @'Foreign.Ptr.Ptr' ()@ value returned
-- by 'castStablePtrToPtr', in this case, is undefined (in particular,
-- it may be 'Foreign.Ptr.nullPtr').  Nevertheless, the call
-- to 'castStablePtrToPtr' is guaranteed not to diverge.
--
freeStablePtr :: StablePtr a -> IO ()
freeStablePtr (StablePtr sp) = ioFromPrim (\_ -> primFreeStableAddr sp)

-- |
-- Coerce a stable pointer to an address. No guarantees are made about
-- the resulting value, except that the original stable pointer can be
-- recovered by 'castPtrToStablePtr'.  In particular, the address may not
-- refer to an accessible memory location and any attempt to pass it to
-- the member functions of the class 'Foreign.Storable.Storable' leads to
-- undefined behaviour.
--
castStablePtrToPtr :: StablePtr a -> Ptr ()
castStablePtrToPtr (StablePtr s) = Ptr (unsafeCoerce s)


-- |
-- The inverse of 'castStablePtrToPtr', i.e., we have the identity
-- 
-- > sp == castPtrToStablePtr (castStablePtrToPtr sp)
-- 
-- for any stable pointer @sp@ on which 'freeStablePtr' has
-- not been executed yet.  Moreover, 'castPtrToStablePtr' may
-- only be applied to pointers that have been produced by
-- 'castStablePtrToPtr'.
--
castPtrToStablePtr :: Ptr () -> StablePtr a
castPtrToStablePtr (Ptr a) = StablePtr (unsafeCoerce a)

instance Eq (StablePtr a) where 
    (StablePtr sp1) == (StablePtr sp2) = sp1 `primEqStableAddr` sp2

%%]
