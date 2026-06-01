{-# LANGUAGE CPP #-}
module Common.SchemaHelpers where

#if !defined(javascript_HOST_ARCH) && !defined(wasm32_HOST_ARCH)
import Database.Beam

type Id a = PrimaryKey a Identity

type MaybeId a = PrimaryKey a (Nullable Identity)
#endif
