{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

#include <bindings.dsl.h>

#include "bindings.h"
#include "buzhash.h"

module Data.BuzHash.Internal.Prim where
#strict_import

#callback_t split_ptr , CUInt -> IO ()

#starttype buzhash_t
#  field window_size    , CUInt
#  field chunk_min_size , CUInt
#  field chunk_max_size , CUInt
#  field mask_bits      , CUInt
#  field mask           , Word32

#  field ringbuf          , Ptr <ringbuf_t>
#  field hash             , Word32
#  field chunk_total_size , CUInt
#stoptype

#starttype ringbuf_t
#  field size , CUInt
#  field idx  , CUInt
#  flexible_array_member buf , Word8
#stoptype

#ccall buzhash_new , CUInt -> CUInt -> CUInt -> CUInt -> IO (Ptr <buzhash_t>)

#ccall buzhash_clone , Ptr <buzhash_t> -> IO (Ptr <buzhash_t>)

#ccall buzhash_free , Ptr <buzhash_t> -> IO ()

#ccall buzhash_process , Ptr <buzhash_t> -> Ptr Word8 -> CUInt -> <split_ptr> \
                      -> IO ()

#globalarray buztable , Word32
