module Database.QUDB.Utils where

{-
The following function is based on GHC's Data.List.sortBy distributed under a
following BSD-style license.

  Copyright 2002, The University Court of the University of Glasgow.
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

  - Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimer.

  - Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.

  - Neither name of the University nor the names of its contributors may be
    used to endorse or promote products derived from this software without
    specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY COURT OF THE UNIVERSITY OF
  GLASGOW AND THE CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
  INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
  UNIVERSITY COURT OF THE UNIVERSITY OF GLASGOW OR THE CONTRIBUTORS BE LIABLE
  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
  DAMAGE.
-}

sortByM :: (Monad m) => (a -> a -> m Ordering) -> [a] -> m [a]
sortByM cmp input = sequences input >>= mergeAll
  where
    sequences (a:b:xs) =
      do rel <- a `cmp` b
         case rel of
           GT -> descending b [a]  xs
           _  -> ascending  b (a:) xs
    sequences xs = return [xs]

    descending a as bs@(b:bs') =
      do rel <- a `cmp` b
         case rel of
           GT -> descending b (a:as) bs'
           _  -> sequences bs >>= return . ((a:as):)
    descending a as bs = sequences bs >>= return . ((a:as):)

    ascending a as bs@(b:bs') =
      do rel <- a `cmp` b
         case rel of
           GT -> sequences bs >>= return . (as [a]:)
           _  -> ascending b (\ys -> as (a:ys)) bs'
    ascending a as bs = sequences bs >>= return . (as [a]:)

    mergeAll [x] = return x
    mergeAll xs  = mergePairs xs >>= mergeAll

    mergePairs (a:b:xs) = do first <- merge a b
                             rest <- mergePairs xs
                             return $ first : rest
    mergePairs xs       = return xs

    merge as@(a:as') bs@(b:bs') =
      do rel <- a `cmp` b
         case rel of
           GT -> merge as  bs' >>= return . (b:)
           _  -> merge as' bs  >>= return . (a:)
    merge [] bs         = return bs
    merge as []         = return as
