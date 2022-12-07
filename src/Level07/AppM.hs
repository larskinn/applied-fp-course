module Level07.AppM
  ( AppM
  , App
  , Env (..)
  , runAppM
  , runApp
  ) where

import           Control.Monad.Except   (MonadError (..))
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader   (MonadReader (..))

import           Data.Text              (Text)

import           Level07.Types          (Conf, FirstAppDB)
import           Level07.Types.Error    (Error)
import Database.SQLite.Simple (Connection)

-- | First, let's clean up our (Conf,FirstAppDB) with an application Env type.
-- We will add a general purpose logging function as well. Remember that
-- functions are values, we're able to pass them around and place them on
-- records like any other type.
data Env = Env

  -- | We will add a function to take some 'Text' input and print it to the
  -- console as a crude form of logging. Construct a function that matches this
  -- type so you can include it when you create the 'Env'.
  { loggingFn :: Text -> App ()

  -- We're able to nest records to keep things neat and tidy.
  , config    :: Conf
  , db        :: FirstAppDB
  }


-- | It would be nice to remove the need to pass around our Env to every
-- function that needs it. Wouldn't it be great to have our functions run where
-- we could simply ask for the current Env?
--
-- We can create this by wrapping a function in a newtype like so:
--
-- This gives us a type that declares this function has access to our Env, and
-- will do something involving IO. It's another form of documentation and type
-- safety. AppM only has one definition and so we can easily understand what it
-- implies when used in our application.
newtype AppM e a = AppM
  { run :: Env -> IO (Either e a)
  }
  -- | Quite often, GHC is able to write the code for us. In this case we just
  -- tell GHC that we want a Functor instance for our newtype, and it is able to
  -- correctly derive what is needed.
  deriving Functor
  -- | We could do this for the rest of these instances, but that would turn
  -- into "magic" what is otherwise straight-forward implementations. You are
  -- here to learn after all.

runAppM :: AppM e a -> Env -> IO (Either e a)
runAppM = (.run)

type App = AppM Error

runApp :: App a -> Env -> IO (Either Error a)
runApp = runAppM

instance Applicative (AppM e) where
  pure :: a -> AppM e a
  pure = AppM . const . pure . Right -- or AppM . pure . pure . pure

  (<*>) :: AppM e (a -> b) -> AppM e a -> AppM e b
  -- (<*>) (AppM f) (AppM x) = AppM $ liftA2 ((<*>) . fmap (<*>)) f x
  (<*>) (AppM f) (AppM x) = AppM $ ((<*>) . fmap ((<*>) . fmap (<*>))) f x

-- Helper
runWithEnv :: (a -> AppM e b) -> Env -> a -> IO (Either e b)
runWithEnv f env = ($ env) . runAppM . f

instance Monad (AppM e) where
  -- | When it comes to running functions in (AppM e) as a Monad, this will take
  -- care of passing the Env from one function to the next whilst preserving the
  -- error handling behaviour.
  (>>=) :: AppM e a -> (a -> AppM e b) -> AppM e b
  (>>=) (AppM x) f = AppM $ \env -> do
    couldFail <- x env
    either (pure . Left) (runWithEnv f env) couldFail


instance MonadError e (AppM e) where
  throwError :: e -> AppM e a
  throwError = AppM . const . pure . Left

  catchError :: AppM e a -> (e -> AppM e a) -> AppM e a
  catchError (AppM x) f = AppM $ \env -> do
    couldFail <- x env
    either (runWithEnv f env) (pure . Right) couldFail

instance MonadReader Env (AppM e) where
  -- Return the current Env from the AppM.
  ask :: AppM e Env
  ask = AppM (pure . Right)

  -- Run a (AppM e) inside of the current one using a modified Env value.
  local :: (Env -> Env) -> AppM e a -> AppM e a
  local f (AppM x) = AppM $ x . f

  -- This will run a function on the current Env and return the result.
  reader :: (Env -> a) -> AppM e a
  reader f = AppM $ pure . Right . f 

instance MonadIO (AppM e) where
  -- Take a type of 'IO a' and lift it into our (AppM e).
  liftIO :: IO a -> AppM e a
  liftIO = AppM . const . fmap pure

-- Move on to ``src/Level07/DB.hs`` after this