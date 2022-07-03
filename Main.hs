module Main where

import Prelude
import Control.Monad

import Web.TheOne
import Web.TheOne.Types

main :: IO ()
main = do
  -- Let's setup our env
  env <- mkTheOneEnv "REDACTED" -- Change this to your api key
  let client = mkTheOneClient env

  -- Fetch all books limit 5
  _books <- runTheOne env (books client (Just 5) Nothing Nothing Nothing)
  print _books

  -- Fetch all books limit 1 page 2
  _booksPage2 <- runTheOne env (books client (Just 1) (Just 2) Nothing Nothing)
  print _booksPage2

  -- Fetch all book by id
  _book <- join <$> traverse (\(Entities{entitiesDocs = (Book{book_id}:_)}) -> runTheOne env (book client book_id)) _books
  print _book

  -- Fetch all book by id chapters
  _bookChapters <- join <$> traverse (\(Entities{entitiesDocs = (Book{book_id}:_)}) -> runTheOne env (bookChapters client book_id)) _books
  print _bookChapters

  -- Fetch all movies limit 5
  _movies <- runTheOne env (movies client (Just 5) Nothing Nothing Nothing)
  print _movies

  -- Fetch all movies limit 1 page 2
  _moviesPage2 <- runTheOne env (movies client (Just 1) (Just 2) Nothing Nothing)
  print _moviesPage2

  -- Fetch all movie by id
  _movie <- join <$> traverse (\(Entities{entitiesDocs = (Movie{movie_id}:_)}) -> runTheOne env (movie client movie_id)) _movies
  print _movie

  -- Fetch all characters limit 5
  _characters <- runTheOne env (characters client (Just 5) Nothing Nothing Nothing)
  print _characters

  -- Fetch all characters limit 1 page 2
  _charactersPage2 <- runTheOne env (characters client (Just 1) (Just 2) Nothing Nothing)
  print _charactersPage2

  -- Fetch all charater by id
  _character <- join <$> traverse (\(Entities{entitiesDocs = (Character{character_id}:_)}) -> runTheOne env (character client character_id)) _characters
  print _character

  pure ()
