module Parser where

import Control.Applicative (Alternative, empty, (<|>), many, some)
import Control.Monad ((>=>), guard)
import Control.Arrow (first)
import Data.Monoid ((<>))
import Data.Char (isSpace)

import Uncons (Uncons(uncons))

newtype ParserT s m a = ParserT (s -> m (a, s))

-- type Parser = ParserT String Maybe

instance (Functor m) => Functor (ParserT s m) where
  fmap f (ParserT pfn) = ParserT $ \xs -> first f <$> pfn xs

instance (Monad m) => Applicative (ParserT s m) where
  pure a = ParserT $ \xs -> pure (a, xs)
  (<*>) parserFn parserA = do
    f <- parserFn
    a <- parserA
    pure $ f a

instance (Monad m) => Monad (ParserT s m) where
  (>>=) (ParserT pfn) f =
    ParserT (pfn >=> (\(a, xs) -> runParser xs (f a)))

instance (Alternative m, Monad m) => Alternative (ParserT s m) where
  empty = ParserT (const empty)
  (<|>) (ParserT a) (ParserT b) = ParserT $ \xs -> a xs <|> b xs

instance (Monoid a, Monad m) => Monoid (ParserT s m a) where
  mempty = pure mempty
  mappend p1 p2 = (<>) <$> p1 <*> p2

item :: (Uncons s c, Alternative m, Monad m) => ParserT s m c
item = ParserT $ \s ->
  case uncons s of
    Nothing     -> empty
    Just (c, s) -> pure (c, s)

satisfy :: (Uncons s c, Alternative m, Monad m) => (c -> Bool) -> ParserT s m c
satisfy predicate = do
  c <- item
  guard $ predicate c
  pure c

one :: (Eq c, Uncons s c, Alternative m, Monad m) => c -> ParserT s m c
one = satisfy . (==)

char :: (Alternative m, Monad m) => Char -> ParserT String m Char
char = one

digit :: (Alternative m, Monad m) => ParserT String m Char
digit = satisfy (\x -> '0' <= x && x <= '9')

string :: (Alternative m, Monad m) => String -> ParserT String m String
string = mapM char

factor :: (Alternative m, Monad m) => Int -> ParserT Int m Int
factor n = ParserT $ \i -> do
  guard $ mod i n == 0
  pure (n, quot i n)

runParser :: s -> ParserT s m a -> m (a, s)
runParser str (ParserT fn) = fn str

separatedByOne :: (Alternative m, Monad m) => ParserT s m a -> ParserT s m b -> ParserT s m [a]
separatedByOne p separator = (:) <$> p <*> many (separator *> p)

separatedBy :: (Alternative m, Monad m) => ParserT s m a -> ParserT s m b -> ParserT s m [a]
separatedBy p separator = separatedByOne p separator <|> pure []

repeatN :: (Alternative m, Monad m) => Int -> ParserT s m a -> ParserT s m [a]
repeatN 0 _ = pure []
repeatN n p = (pure <$> p) <> repeatN (n - 1) p

within :: (Alternative m, Monad m) => ParserT s m b -> ParserT s m b -> ParserT s m a -> ParserT s m a
within before after p = before *> p <* after

singleSpace :: (Alternative m, Monad m) => ParserT String m Char
singleSpace = satisfy isSpace

space :: (Alternative m, Monad m) => ParserT String m String
space = many singleSpace
