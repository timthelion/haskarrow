This is an implementation of the notorious glade tutorial, in haskarrow:

The origional tutorial can be found here <http://projects.haskell.org/gtk2hs/docs/tutorial/glade/>

>module SimpleWindow where
>import Graphics.UI.Gtk
>import Graphics.UI.Gtk.Builder
>import System.Exit
>import Control.Concurrent
>import Control.Concurrent.MVar
>import Control.Monad.IO.Class


>builder :: FilePath -> IO Builder
>builder gladeFile << do
> initGUI
> builder' <- builderNew
> builderAddFromFile builder' gladeFile
> forkOS mainGUI
> return builder'

>window :: Builder -> IO Window
>window builder <<
> postGUISync $ do
>  window' <- builderGetObject builder castToWindow "window1"
>  widgetShowAll window'
>  return window'

>exitButton :: Builder -> IO Button
>exitButton builder <<
> postGUISync $ do
>  exitButton' <- builderGetObject builder castToButton "exitButton"
>  return exitButton'

>exitButtonOnClick :: Button -> MVar ExitCode -> IO ()
>exitButtonOnClick exitButton exitMVar <<
> postGUIAsync $ do
>  exitButton `on` buttonActivated $ do
>   liftIO $ do
>    putMVar exitMVar ExitSuccess
>  return ()

>continueButton :: Builder -> IO Button
>continueButton builder <<
> postGUISync $ do
>  continueButton' <- builderGetObject builder castToButton "continueButton"
>  return continueButton'

>continueButtonOnClick :: Button -> Label -> Entry -> UpdateLabelType ->  IO ()
>continueButtonOnClick continueButton label entry updateLabel <<
> postGUIAsync $ do
>  continueButton `on` buttonActivated $ do
>   liftIO $ do
>    updateLabel entry label
>  return ()

>label :: Builder -> IO Label
>label builder <<
> postGUISync $ do
>  label' <- builderGetObject builder castToLabel "label"
>  return label'

>type UpdateLabelType =  Entry -> Label -> IO ()
>updateLabel :: UpdateLabelType
>updateLabel entry label = do
> name <- get entry entryText
> set label [ labelText := "Hello " ++ name ]
> return ()

>entry :: Builder -> IO Entry
>entry builder <<
> postGUISync $ do
>  entry' <- builderGetObject builder castToEntry "entry"
>  return entry'

>entryOnKeyPress :: Entry -> Label -> UpdateLabelType -> IO ()
>entryOnKeyPress entry label updateLabel << do
> postGUISync $ do
>  entry `on` keyPressEvent $ do
>   key  <- eventKeyName
>   case key of
>    "Return" -> do
>     liftIO $ updateLabel entry label
>     return True
>    _ -> return False
>  return ()

>gladeFile :: IO FilePath
>gladeFile << return "SimpleWindow.glade"

>exitMVar :: IO (MVar ExitCode)
>exitMVar << newEmptyMVar

>exit :: MVar ExitCode -> IO (IO ())
>exit exitMVar << return $ do
> exitCode <- takeMVar exitMVar
> exitWith exitCode
