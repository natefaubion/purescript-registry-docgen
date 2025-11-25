module Github
  ( GithubReadmeParams
  , githubReadme
  ) where

import Prelude

import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)

type GithubReadmeParams =
  { token :: String
  , user :: String
  , repo :: String
  , ref :: String
  }

type GithubReadmeResult =
  { name :: String
  , content :: String
  , ok :: Boolean
  , status :: Int
  }

foreign import githubReadmeImpl :: GithubReadmeParams -> EffectFnAff GithubReadmeResult

githubReadme :: GithubReadmeParams -> Aff GithubReadmeResult
githubReadme = fromEffectFnAff <$> githubReadmeImpl
