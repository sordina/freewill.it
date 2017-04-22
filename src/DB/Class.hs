
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DB.Class where

import API

-- | Individual Components of Database Functionality
--
class Name   x m | x -> m where name   :: x -> Choice               -> m Choice
class View   x m | x -> m where view   :: x -> ChoiceID             -> m ChoiceAPIData
class Add    x m | x -> m where add    :: x -> ChoiceID -> Option   -> m Option
class Choose x m | x -> m where choose :: x -> ChoiceID -> OptionID -> m Decision
class List   x m | x -> m where list   :: x                         -> m [Choice]

-- | Convenience Class Intended for Most Higer-Level Application Routes
--
class (Name x m, View x m, Add x m, Choose x m, List x m) => Database x m where
