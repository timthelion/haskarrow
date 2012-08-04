>module SimpleWindow where
>import Graphics.UI.Gtk
>import Graphics.UI.Gtk.Builder
>import Control.Concurrent

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

>gladeFile :: IO FilePath
>gladeFile << return "SimpleWindow.glade"
