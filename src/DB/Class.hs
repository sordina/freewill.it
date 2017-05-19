
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DB.Class where

import Data

-- | Individual Components of Database Functionality
--
class Name     db m | db -> m where name     :: db -> UserID -> Choice               -> m Choice
class View     db m | db -> m where view     :: db -> UserID -> ChoiceID             -> m ChoiceAPIData
class Add      db m | db -> m where add      :: db -> UserID -> ChoiceID -> Option   -> m Option
class Choose   db m | db -> m where choose   :: db -> UserID -> ChoiceID -> OptionID -> m Decision
class List     db m | db -> m where list     :: db -> UserID                         -> m [Choice]
class Register db m | db -> m where register :: db -> String -> String               -> m UserID
class Login    db m | db -> m where login    :: db -> String -> String               -> m UserID

-- | Convenience Class Intended for Most Higer-Level Application Routes
--
class ( Name     db m
      , View     db m
      , Add      db m
      , Choose   db m
      , List     db m
      , Register db m
      , Login    db m
      )
      => Database db m where
