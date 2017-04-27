
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DB.Class where

import API

-- | Individual Components of Database Functionality
--
class Name   db m | db -> m where name   :: db -> Choice               -> m Choice
class View   db m | db -> m where view   :: db -> ChoiceID             -> m ChoiceAPIData
class Add    db m | db -> m where add    :: db -> ChoiceID -> Option   -> m Option
class Choose db m | db -> m where choose :: db -> ChoiceID -> OptionID -> m Decision
class List   db m | db -> m where list   :: db                         -> m [Choice]

-- | Convenience Class Intended for Most Higer-Level Application Routes
--
class (Name db m, View db m, Add db m, Choose db m, List db m) => Database db m where
