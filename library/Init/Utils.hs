{-# language CPP #-}

-- | This is a module of custom "Prelude" code.
-- This module gets reexported by projects "Prelude".
module Init.Utils
  ( stub
  , pass
  , dup
  , both
  , mapPair
  , nestM
  , applyAll
  , traverse2

  , whenTrue
  , whenFalse
  , whenJust
  , isInhabited
  , handlePresence
  , handleTextPresence

  , Path(..)
  , isAbsolute
  , (</>)
  , joinPath
  , splitDirectories
  , takeDirectory
  , takeFileName
  , takeBaseName
  , takeExtension
  , takeExtensions
  , addExtension
  , dropExtensions
  , replaceExtension
  , readFile
#if ENABLE_TRACING
  , module X
#else
  , trace
  , traceM
#endif
  )
 where

import           Relude                  hiding ( pass
                                                , force
                                                , readFile
                                                , whenJust
                                                , whenNothing
                                                , trace
                                                , traceM
                                                )

import           Data.Data                      ( Data )
import qualified Data.Text                     as Text
import qualified System.FilePath               as FilePath
import           Control.Monad.List             ( foldM )

#if ENABLE_TRACING
import qualified Relude.Debug                 as X
#else
-- Well, since it is currently CPP intermingled with Debug.Trace, required to use String here.
trace :: String -> a -> a
trace = const id
{-# inline trace #-}
traceM :: Monad m => String -> m ()
traceM = const stub
{-# inline traceM #-}
#endif

-- * Helpers

-- After migration from the @relude@ - @relude: pass -> stub@
-- | @pure mempty@: Short-curcuit, stub.
stub :: (Applicative f, Monoid a) => f a
stub = pure mempty
{-# inline stub #-}

-- | Alias for 'stub', since "Relude" has more specialized @pure ()@.
pass :: (Applicative f) => f ()
pass = stub
{-# inline pass #-}

-- | Duplicates object into a tuple.
dup :: a -> (a, a)
dup x = (x, x)
{-# inline dup #-}

-- | Apply a single function to both components of a pair.
--
-- > both succ (1,2) == (2,3)
--
-- Taken From package @extra@
both :: (a -> b) -> (a, a) -> (b, b)
both f (x,y) = (f x, f y)
{-# inline both #-}

-- | Gives tuple laziness.
--
-- Takem from @utility-ht@.
mapPair :: (a -> c, b -> d) -> (a,b) -> (c,d)
mapPair ~(f,g) ~(a,b) = (f a, g b)
{-# inline mapPair #-}

nestM
  :: Monad m
  => Int -- ^ Recursively apply 'Int' times
  -> (a -> m a) -- ^ function (Kleisli arrow).
  -> a -- ^ to value
  -> m a -- ^ & join layers of 'm'
nestM 0 _ x = pure x
nestM n f x =
  foldM (const . f) x $ replicate @() n mempty
{-# inline nestM #-}

-- | In `foldr` order apply functions.
applyAll :: Foldable t => t (a -> a) -> a -> a
applyAll = flip (foldr id)

traverse2
  :: ( Applicative m
     , Applicative n
     , Traversable t
     )
  => ( a
     -> m (n b)
     ) -- ^ Run function that runs 2 'Applicative' actions
  -> t a -- ^ on every element in 'Traversable'
  -> m (n (t b)) -- ^ collect the results.
traverse2 f x = sequenceA <$> traverse f x


-- * Eliminators

whenTrue :: (Monoid a)
  => a -> Bool -> a
whenTrue =
  bool
    mempty
{-# inline whenTrue #-}

whenFalse :: (Monoid a)
  => a  -> Bool  -> a
whenFalse f =
  bool
    f
    mempty
{-# inline whenFalse #-}

whenJust
  :: Monoid b
  => (a -> b)
  -> Maybe a
  -> b
whenJust =
  maybe
    mempty
{-# inline whenJust #-}

isInhabited :: Foldable t => t a -> Bool
isInhabited = not . null
{-# inline isInhabited #-}


-- | 'maybe'-like eliminator, for foldable empty/inhabited structures.
handlePresence :: Foldable t => b -> (t a -> b) -> t a -> b
handlePresence d f t =
  bool
    d
    (f t)
    (isInhabited t)
{-# inline handlePresence #-}

handleTextPresence
  :: a -> (Text -> a) -> Text -> a
handleTextPresence e f t =
  bool
    e
    (f t)
    (not $ Text.null t)


-- * Path

-- | Explicit type boundary between FilePath & String.
newtype Path = Path FilePath
  deriving
    ( Eq, Ord, Generic
    , Typeable, Data
    , Show, Read, Hashable
    , Semigroup, Monoid
    )

instance ToText Path where
  toText = toText @String . coerce

instance IsString Path where
  fromString = coerce

-- ** Path functions

-- | This set of @Path@ funcs is to control system filepath types & typesafety and to easily migrate from FilePath to anything suitable (like @path@ or so).

-- | 'Path's 'FilePath.isAbsolute'.
isAbsolute :: Path -> Bool
isAbsolute = coerce FilePath.isAbsolute

-- | 'Path's 'FilePath.(</>)'.
(</>) :: Path -> Path -> Path
(</>) = coerce (FilePath.</>)
infixr 5 </>

-- | 'Path's 'FilePath.joinPath'.
joinPath :: [Path] -> Path
joinPath = coerce FilePath.joinPath

-- | 'Path's 'FilePath.splitDirectories'.
splitDirectories :: Path -> [Path]
splitDirectories = coerce FilePath.splitDirectories

-- | 'Path's 'FilePath.takeDirectory'.
takeDirectory :: Path -> Path
takeDirectory = coerce FilePath.takeDirectory

-- | 'Path's 'FilePath.takeFileName'.
takeFileName :: Path -> Path
takeFileName = coerce FilePath.takeFileName

-- | 'Path's 'FilePath.takeBaseName'.
takeBaseName :: Path -> String
takeBaseName = coerce FilePath.takeBaseName

-- | 'Path's 'FilePath.takeExtension'.
takeExtension :: Path -> String
takeExtension = coerce FilePath.takeExtensions

-- | 'Path's 'FilePath.takeExtensions'.
takeExtensions :: Path -> String
takeExtensions = coerce FilePath.takeExtensions

-- | 'Path's 'FilePath.addExtensions'.
addExtension :: Path -> String -> Path
addExtension = coerce FilePath.addExtension

-- | 'Path's 'FilePath.dropExtensions'.
dropExtensions :: Path -> Path
dropExtensions = coerce FilePath.dropExtensions

-- | 'Path's 'FilePath.replaceExtension'.
replaceExtension :: Path -> String -> Path
replaceExtension = coerce FilePath.replaceExtension

-- | 'Path's 'FilePath.readFile'.
readFile :: MonadIO m => Path -> m Text
readFile = readFileText . coerce
