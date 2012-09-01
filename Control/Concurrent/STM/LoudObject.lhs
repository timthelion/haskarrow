>module Control.Concurrent.STM.LoudObject where

>import Control.Concurrent.STM.TChan
>import Control.Concurrent.STM
>import Control.Concurrent.MVar

>newtype SourceID = SourceID Int

>newtype SourceObject a = (SourceID ,TChan LoudObjectSourceUpdate, TChan a, MVar a)

>newtype UpdateID = UpdateID Int

>newtype LoudObjectSourceUpdate = (UpdateID,[SourceID])

>type LoudObject a = (TChan a,MVar a)

>newtype LoudObjectS a =
> LoudObjectS (TChan a, MVar a, Maybe a)

>lov :: LoudObjectS a -> a
>lov (LoudObjectS (_,_,Just v))  = v
>lov (LoudObjectS (_,_,Nothing)) =
> error "State of this Loud Object has not been initialized."

>newLoudObject
>  ::
> a ->
> IO
>  (LoudObject a)

>newLoudObject
> initialValue
>  =
> do
>  tchan <- newTChanIO
>  mvar  <- newMVar initialValue
>  atomically
>   $ writeTChan tchan initialValue
>  return (tchan, mvar)

>readLoudObjectSQue
>  ::
> LoudObjectS a ->
> (STM (IO (LoudObjectS a)),IO (LoudObjectS a))

>readLoudObjectSQue
> los@(LoudObjectS (tchan,mvar,maybeValue))
>  =
> (case maybeValue of
>      Just _ -> do
>       v <- readTChan tchan;
>       return (return $ LoudObjectS (tchan,mvar,Just v))
>      Nothing ->
>       return grabValueFromMVar,
>  case maybeValue of
>   Just v  -> return los
>   Nothing -> grabValueFromMVar)
> where
>  grabValueFromMVar =
>   do
>    v <- readMVar mvar
>    return $ LoudObjectS (tchan,mvar,Just v)

>modifyLoudObject
>  ::
> LoudObject a ->
> (a -> a)     ->
> IO ()

>modifyLoudObject
> (tchan,mvar)
> f
>  = do
> v <- takeMVar mvar
> v' <- return $ f v ;
> atomically
>  $ writeTChan
>     tchan
>     v'
> putMVar mvar v'

>modifyLoudObjectIO
>  ::
> LoudObject a ->
> (a -> IO a)  ->
> IO ()

>modifyLoudObjectIO
> (tchan,mvar)
> f
>  = do
> v <- takeMVar mvar
> v' <- f v ;
> atomically
>  $ writeTChan
>     tchan
>     v'
> putMVar mvar v'

>withTChan
>  ::
> TChan a         ->
> (TChan a -> b)  ->
> IO b
>withTChan
> tchan
> f
>   =
> do
>  dupedChan
>    <- atomically
>        $ dupTChan tchan
>  return
>   $ f dupedChan

>withLoudObjectS
>  ::
> LoudObject a          ->
> (LoudObjectS a -> d)  ->
> IO d

>withLoudObjectS
> (tchan,mvar)
> f
>   =
> do
>  dupedChan
>    <- atomically
>        $ dupTChan tchan
>  return
>   $ f (LoudObjectS (dupedChan,mvar,Nothing))

>stmStateOnion
>  ::
> (STM   a  )->
> a          ->
> (STM   b  )->
> b          ->
> (STM  (a,b),(a,b))

>stmStateOnion
> stm1
> oldVal1
> stm2
> prevTup
>   =
> (((do 
>   val1' <- stm1
>   return (val1',prevTup)) 
>    `orElse` 
>  (do 
>   val2' <- stm2
>   return
>    (oldVal1,val2'))),
>  (oldVal1,prevTup))

>awaitUpdate
>  ::
> (STM a,b) ->
> STM a

>awaitUpdate
> (stm,_)
>   =
> stm

>(<|&|>)
>  ::
> (STM a,a) ->
> (STM b,b) ->
> (STM (a,b),(a,b))

>(<|&|>)
> (thisSTM,oldValue)
> (prevSTM,prevSTMTup)
>   =
> stmStateOnion
>  thisSTM
>  oldValue
>  prevSTM
>  prevSTMTup

