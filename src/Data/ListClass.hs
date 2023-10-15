module Data.ListClass where

class IsList f where
  -- The elementary list operations.
  -- Roughly taken from the minimal primitives for a Lisp linked list.
  cons    :: a -> f a -> f a
  car     :: f a -> a
  cdr     :: f a -> f a
  empty   :: f a
  isEmpty :: f a -> Bool

  -- Derived operations.
  fromList :: [a] -> f a
  fromList = foldr cons empty

  toList :: f a -> [a]
  toList xs
    | isEmpty xs = []
    | otherwise  = car xs : toList (cdr xs)

  coerce :: IsList g => f a -> g a
  coerce = fromList . toList

  normalize :: f a -> f a
  normalize = coerce

  single :: a -> f a
  single = (`cons` empty)

  isSingle :: f a -> Bool
  isSingle xs = not (isEmpty xs) && isEmpty (cdr xs)

  append :: f a -> f a -> f a
  append xs ys
    | isEmpty xs = ys
    | isEmpty ys = xs
    | otherwise  = cons (car xs) $ append (cdr xs) ys

  snoc :: f a -> a -> f a
  snoc = (. single) . append

  size :: f a -> Int
  size
    = go 0
      where
        go acc xs
          | isEmpty xs = acc
          | otherwise  = go (acc + 1) (cdr xs)

  atIndex :: f a -> Int -> a
  atIndex xs n
    | isEmpty xs = error "atIndex: Index too large."
    | n == 0     = car xs
    | otherwise  = atIndex (cdr xs) (n - 1)

  prefixN :: Int -> f a -> f a
  prefixN n xs
    | isEmpty xs || n == 0 = empty
    | otherwise            = car xs `cons` prefixN (n - 1) (cdr xs)

  subPrefixN :: Int -> f a -> f a
  subPrefixN n xs
    | isEmpty xs || n == 0 = xs
    | otherwise            = subPrefixN (n - 1) (cdr xs)

  -- Get the list, without the last item.
  butLast :: f a -> f a
  butLast xs
    | isEmpty xs  = error "butLast: Empty list."
    | isSingle xs = empty
    | otherwise   = cons (car xs) (butLast (cdr xs))

  -- Get the last item of a list of type l.
  final :: f a -> a
  final xs
    | isEmpty xs  = error "last: Empty list."
    | isSingle xs = car xs
    | otherwise   = final (cdr xs)

instance IsList [] where
  cons = (:)
  car = head
  cdr = tail
  empty = []
  isEmpty = null

  fromList = id
  toList = id
  normalize = id

  single = pure
  isSingle [_] = True
  isSingle _   = False

  append = (++)
  size = length
  atIndex = (!!)
  prefixN = take
  subPrefixN = drop
  butLast = init
  final = last

