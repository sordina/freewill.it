
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DB.Class where

import Data

-- | Individual Components of Database Functionality
--
class Name     db m | db -> m where name     :: db -> UserID -> Choice               -> m Choice
class View     db m | db -> m where view     :: db -> UserID -> ChoiceID             -> m ChoiceAPIData
class Share    db m | db -> m where share    :: db -> UserID -> ChoiceID             -> m Choice
                                    hide     :: db -> UserID -> ChoiceID             -> m Choice
class Add      db m | db -> m where add      :: db -> UserID -> ChoiceID -> Option   -> m Option
class Choose   db m | db -> m where choose   :: db -> UserID -> ChoiceID -> OptionID -> m Decision
class List     db m | db -> m where list     :: db -> UserID                         -> m [Choice]
class Search   db m | db -> m where search   :: db -> UserId -> ChoiceQuery          -> m [Choice]
class Me       db m | db -> m where me       :: db -> UserID                         -> m User
class Register db m | db -> m where register :: db -> String -> Password             -> m User
class Login    db m | db -> m where login    :: db -> String -> Password             -> m User

-- | Convenience Class Intended for Most Higer-Level Application Routes
--
-- TODO: Add Search, or Merge into List
--
class ( Name     db m
      , View     db m
      , Add      db m
      , Choose   db m
      , List     db m
      , Me       db m
      , Register db m
      , Login    db m
      , Share    db m
      )
      => Database db m where
