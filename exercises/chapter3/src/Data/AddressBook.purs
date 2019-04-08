module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head, groupBy)
import Data.List.NonEmpty as NonEmpty
import Data.Maybe (Maybe, isJust)

type Address =
  { street :: String
  , city   :: String
  , state  :: String
  }

type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }

type AddressBook = List Entry

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <> addr.city <> ", " <> addr.state

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <> entry.firstName <> ": " <> showAddress entry.address

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

find :: (Entry -> Boolean) -> AddressBook -> Maybe Entry
find predicate book = head $ filter predicate book

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = find filterEntry
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.firstName == firstName && entry.lastName == lastName


whoLivesAt :: String -> AddressBook -> Maybe Entry
whoLivesAt street = find filterEntry
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.address.street == street

exists :: String -> String -> AddressBook -> Boolean
exists firstName lastName = isJust <<< findEntry firstName lastName

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates book = (groupBy name book) <#> NonEmpty.head
  where
    name :: Entry -> Entry -> Boolean
    name a b = a.firstName == b.firstName && a.lastName == b.lastName
