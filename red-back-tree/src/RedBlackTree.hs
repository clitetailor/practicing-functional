module RedBlackTree where

import Data.Unique (Unique, hashUnique, newUnique)
import Prelude hiding (id)

data Color = Red | Black deriving (Show, Eq)

data Node = Node
  { id :: Unique,
    key :: Int,
    color :: Color,
    parent :: Maybe Node,
    left :: Maybe Node,
    right :: Maybe Node
  }
  deriving (Show, Eq)

instance Show Unique where
  show unique = "Unique " <> (show $ hashUnique unique)

newNode :: Color -> Int -> IO Node
newNode color key = do
  id <- newUnique

  return
    Node
      { id = id,
        key = key,
        color = color,
        parent = Nothing,
        left = Nothing,
        right = Nothing
      }

insertNode :: Node -> Node -> Node
insertNode curNode newNode =
  if key newNode > key curNode
    then case right curNode of
      Just rNode ->
        curNode
          { right = Just $ insertNode rNode newNode
          }
      Nothing ->
        curNode
          { right = Just $ newNode
          }
    else case left curNode of
      Just lNode ->
        curNode
          { left = Just $ insertNode lNode newNode
          }
      Nothing ->
        curNode
          { left = Just $ newNode
          }

getParent :: Node -> Maybe Node
getParent node = parent node

getGrandParent :: Node -> Maybe Node
getGrandParent node = do
  pNode <- parent node

  parent pNode

getSibling :: Node -> Maybe Node
getSibling node = do
  pNode <- parent node
  lNode <- left pNode

  if node == lNode
    then right pNode
    else left pNode

getUncle :: Node -> Maybe Node
getUncle node = do
  pNode <- getParent node

  getSibling pNode
