
data Free f a = Pure a | Free (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap fn (Pure x) = Pure (fn x)
  -- fmap fn (Free g) = Free (fmap fn <$> g)
  fmap fn (Free g) = Free (fmap (fmap fn) g)

instance Functor f => Applicative (Free f) where
  pure = Pure
  Pure f <*> as  = fmap f as
  Free faf <*> as  = Free (fmap (<*> as) faf)

instance Functor f => Monad (Free f) where
  return = Pure
  Pure x >>= f = f x
  -- Free g >>= f = Free ((>>= f) <$> g)
  Free g >>= f = Free (fmap (>>= f) g)
