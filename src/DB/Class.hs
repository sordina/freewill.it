
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DB.Class where

import API

class View x m | x -> m where
  view :: x -> ID -> m ChoiceAPIData

class Add x m | x -> m where
  add :: x -> ID -> Option -> m Option

class Choose x m | x -> m where
  choose :: x -> ID -> ID -> m Decision

class List x m | x -> m where
  list :: x -> m [Choice]
