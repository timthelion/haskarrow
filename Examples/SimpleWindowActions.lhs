>module SimpleWindowActions where
>import Graphics.UI.Gtk

>updateLabel :: Entry -> Label -> IO ()
>updateLabel entry label = do
> name <- get entry entryText
> set label [ labelText := "Hello " ++ name ]
> return ()

