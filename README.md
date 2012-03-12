Haskell-Libnotify
=========================

What is it?
-----------
Haskell-Libnotify is usable (I hope) binding to [libnotify][1] library.

 [1]: http://developer.gnome.org/libnotify/

How to use it?
--------------
Haskell-libnotify provides ways to work with: simple and more complicated.  
You would prefer simple way if all you want is one notification as a result of some event:

    -- displays notification with title "Hello world!", body "Want some notifications?", question icon and then exits.
    import System.Libnotify

    main :: IO ()
    main = oneShot "Hello world!" "Want some notifications?" "dialog-question" Nothing

However, if you want some nontrivial behaviour, such as updating already displayed notification or closing it, you would use:

    -- displays notification with title "Hello world!", body "Want some notifications?", question icon for one second
    -- then displays notification with title "Hello world!", body "Here it is!", information icon for one second and exits

    import System.Libnotify
    import Control.Concurrent (threadDelay)
    import Control.Monad.Trans (liftIO)

    main :: IO ()
    main = withNotifications Nothing $
             do let title = "Hello world!"
                    body = "Wnat some notifications?"
                    icon = "dialog-question"
                new title body icon $ do
                  render
                  liftIO $ threadDelay 1000000
                  update Nothing (Just "Here it is!") (Just "dialog-information")
                  render
                  liftIO $ threadDelay 1000000
                  close

If you can use one notification in different program places, but API isn't stable in this part yet.

    -- same example with session continued
    import System.Libnotify
    import Control.Concurrent (threadDelay)

    main :: IO ()
    main = withNotifications Nothing $
             do let title = "Hello world!"
                    body = "Wnat some notifications?"
                    icon = "dialog-question"
                n <- new title body icon render
                threadDelay 1000000
                continue title body icon n $ update Nothing (Just "Here it is!") (Just "dialog-information") >> render
                threadDelay 1000000
                continue title body icon n close

Actually, that's all.
