>{-# LANGUAGE NamedFieldPuns #-}
>module Control.Concurrent.STM.LoudObject where

>import Control.Concurrent
>import Control.Concurrent.STM.TChan
>import Control.Concurrent.STM
>import Control.Concurrent.MVar
>import Data.List (sortBy)

>newtype SourceID = SourceID Int
> -- ^ SourceID -1 is reserved for the case of a touch or another event which would cause a flush of an objects influence channels.

>data LoudSource a = LoudSource{
> sourceID         :: SourceID                     ,
> updatesChan      :: TChan LoudSourceUpdate       ,
> valuesChan       :: TChan a                      ,
> currentValueMVar :: MVar a}

>data LoudSourceUpdate = LoudSourceUpdate{
> updatedSources :: [SourceID]}

>data LoudListener a = LoudListener{
> listenerValuesChan :: TChan a,
> listenerTouchChan  :: TChan ()}

>newLoudSource ::
> MVar SourceID ->
> -- ^ This is our global ID counter. OMG mutability when it's not necessary...  <-- We're acutally less likely to encounter bugs using mutability here than using some kind of imutable state.  We want to be SURE we have a global effect and that the ID's are globally unique(Global as in, to the extent of our haskarrow module).
> a ->
> -- ^ Initial value of our loud source.
> IO (LoudSource a)

>newLoudSource
> idCounterMVar
> initialValue
>  =
> do
>  mySourceID <-
>   modifyMVar
>    idCounterMVar
>    (\(SourceID id)->return (SourceID (id+1),SourceID id))
>  newLoudSource'
>   mySourceID
>   initialValue

>newLoudSource'
> nextID
> initialValue
>  =
> do
>  myValuesChan <-
>   newBroadcastTChanIO

>  updatesChan <-
>   newBroadcastTChanIO

>  myCurrentValueMVar <-
>   newMVar initialValue

>  return $
>   LoudSource{
>    sourceID         = nextID        ,
>    updatesChan      = updatesChan   ,
>    valuesChan       = myValuesChan  ,
>    currentValueMVar = myCurrentValueMVar
>    }

>newLoudListener ::
> IO (LoudListener a)
>newLoudListener
>  =
> do
>  vchan <- 
>   newBroadcastTChanIO
>  touchChan <-
>   newTChanIO
>  return $ LoudListener vchan touchChan

>setLoudListenerValue ::
> LoudListener a ->
> a ->
> IO ()

>setLoudListenerValue
> (LoudListener tchan _)
> value
>  =
> do
>  atomically
>   $ do
>     writeTChan
>      tchan
>      value

>doLoudSourceModification
>  ::
> [(SourceID,[SourceID]->IO())] ->
> IO ()
>doLoudSourceModification
> pairs
>  =
> let
>  (ids,updates)=unzip pairs
>  sortedIDs =
>   sortBy
>    (\(SourceID id) (SourceID id1)-> id `compare` id1)
>    ids
> in do
> mapM
>  (\update ->
>     update sortedIDs)
>  updates
> return ()

>modifyLoudSource
>  ::
> LoudSource a ->
> (a -> a)     ->
> (SourceID, [SourceID] ->IO ())

>modifyLoudSource
> loudSource
> f
>  =
> modifyLoudSourceIO loudSource (\x -> return $ f x)

>modifyLoudSourceIO
>  ::
> LoudSource a ->
> (a -> IO a)     ->
> (SourceID, [SourceID] ->IO ())

>modifyLoudSourceIO
> LoudSource{
>  sourceID         = sourceID         ,
>  updatesChan      = updatesChan      ,
>  valuesChan       = valuesChan       ,
>  currentValueMVar = currentValueMVar
>  }
> f
>  = 
> (sourceID,
> (\sources -> do
> v <- takeMVar currentValueMVar
> v' <- f v ;
> atomically
>  $ do
>     writeTChan
>      valuesChan
>      v'
>     writeTChan
>      updatesChan
>      (LoudSourceUpdate sources)
> putMVar currentValueMVar v'))

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

>withLoudSource
>  ::
> LoudSource a             ->
> ((LoudSource a,a) -> d)  ->
> IO d

>withLoudSource
> LoudSource{
>  sourceID         = sourceID         ,
>  updatesChan      = updatesChan      ,
>  valuesChan       = valuesChan       ,
>  currentValueMVar = currentValueMVar
>  }
> f
>   =
> do

>  dupedUpdatesChan
>    <- atomically
>        $ dupTChan updatesChan

>  dupedValuesChan
>    <- atomically
>        $ dupTChan valuesChan

>  currentValue <-
>   readMVar currentValueMVar

>  return
>   $ f (LoudSource {
>         sourceID         = sourceID         ,
>         updatesChan      = dupedUpdatesChan ,
>         valuesChan       = dupedValuesChan  ,
>         currentValueMVar = currentValueMVar
>         },
>        currentValue)

>withLoudListener
>   ::
> LoudListener a ->
> ((LoudListener a,a) -> b) ->
> IO b

>withLoudListener
> ll@(LoudListener tchan touchChan)
> f
>  =
> do
>  dupedListenerChan
>    <- atomically
>        $ dupTChan tchan

>  touchLoudListener ll;

>  currentValue <- atomically
>   $ readTChan
>      dupedListenerChan;

>  return
>   $ f (LoudListener dupedListenerChan touchChan,currentValue)

>touchLoudListener ::
> LoudListener a ->
> IO ()
>touchLoudListener
> (LoudListener _ touchChan)
>  =
> atomically
>  $ writeTChan
>     touchChan
>     ()

>listenForTouch ::
> LoudListener a ->
> STM (SourceID,LoudSourceUpdate)
>listenForTouch
> (LoudListener _ touchChan)
>  =
> packTouch $ readTChan touchChan

>peekInfluence ::
> LoudListener a ->
> STM (SourceID,LoudSourceUpdate)
>peekInfluence
> (LoudListener vChan _)
>  =
> packTouch
>  $ peekTChan vChan

>packTouch ::
> STM a ->
> STM (SourceID,LoudSourceUpdate)
>packTouch
> stm
>  =
> do
>  _ <- stm
>  return (SourceID (-1),LoudSourceUpdate [SourceID (-1)])

> -- | If the TChan has contents, empty it.  Return either the last value put into the TChan or the constant value given.
>flushTChan ::
> TChan a ->
> a       ->
> IO a
>flushTChan
> tchan
> oldValue
>  =
> do
>  result <- atomically
>   $ tryReadTChan tchan
>  case result of
>   Just newValue ->
>    flushTChan
>     tchan
>     newValue
>   Nothing -> return oldValue
