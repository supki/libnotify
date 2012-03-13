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

```haskell
-- displays notification with title "Hello world!", body "Want some notifications?", question icon and then exits.
import Control.Monad (void)
import System.Libnotify

main :: IO ()
main = void $ oneShot "Hello world!" "Want some notifications?" "dialog-question" Nothing
```

However, if you want some nontrivial behaviour, such as updating already displayed notification or closing it, you would use:

```haskell
-- displays notification with title "Hello world!", body "Want some notifications?", question icon for one second
-- then displays notification with title "Hello world!", body "Here it is!", information icon for one second and exits

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Control.Monad.Trans (liftIO)
import System.Libnotify

main :: IO ()
main = void $ withNotifications Nothing $
         do let title = "Hello world!"
                body = "Want some notifications?"
                icon = "dialog-question"
            new title body icon $ do
              render
              liftIO $ threadDelay 1000000
              update Nothing (Just "Here it is!") (Just "dialog-information")
              render
              liftIO $ threadDelay 1000000
              close
```

You can even use one notification in different program places:

```haskell
-- same example with session continued
import Control.Concurrent (threadDelay)
import Control.Monad (void)
import System.Libnotify

main :: IO ()
main = void $ withNotifications Nothing $
         do let title = "Hello world!"
                body = "Want some notifications?"
                icon = "dialog-question"
            Right n <- new title body icon render
            threadDelay 1000000
            continue n $ update Nothing (Just "Here it is!") (Just "dialog-information") >> render
            threadDelay 1000000
            continue n close
```

![That's all folks!](http://vsegda.budueba.com/img/b5646a36b80cedc99a2c5fee762d4347.jpg)
