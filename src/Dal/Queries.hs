module Dal.Queries where

import qualified Dal.Model as Model
import Data.Aeson as Aeson
import Data.Aeson.Types (parse)
import qualified Data.ByteString.Char8 as BS
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Statement as Statement

-- Decoders

stringColumn :: Decoders.Row String
stringColumn = fmap show (Decoders.column (Decoders.nonNullable Decoders.text))

jsonColumn :: Decoders.Row Aeson.Value
jsonColumn = Decoders.column (Decoders.nonNullable Decoders.json)

idColumn :: Decoders.Row (Model.ID a)
idColumn = fmap (Model.ID . fromIntegral) (Decoders.column (Decoders.nonNullable Decoders.int4))

userDecoder :: Decoders.Result [Model.User]
userDecoder =
  fmap
    (map (\(i, n, h) -> Model.User i n h))
    ( Decoders.rowList $
        (,,)
          <$> idColumn
          <*> stringColumn
          <*> stringColumn
    )

decodeError :: Aeson.Value -> Aeson.Result [Model.Error]
decodeError = parse parseJSON

-- TODO: by default it returns empty error list if it fails to parse, should
-- rewrite it with monad in future, but it is pain in the ass and needs it's
-- own research
phraseDecoder :: Decoders.Result [Model.Phrase]
phraseDecoder = fmap (map makePhrase) statement
 where
  statement =
    Decoders.rowList $
      (,,,,)
        <$> idColumn
        <*> stringColumn
        <*> jsonColumn
        <*> idColumn
        <*> idColumn
  makePhrase (i, t, e, g, a) = Model.Phrase i t pe g a
   where
    pe = case decodeError e of
      Aeson.Success s -> s
      Aeson.Error er -> [Model.Error (show er) "t"]

-- Queries

selectUsers :: Statement.Statement () [Model.User]
selectUsers = Statement.Statement (BS.pack query) Encoders.noParams userDecoder True
 where
  query = "SELECT * FROM users" -- TODO: make this typed

selectPhrases :: Statement.Statement () [Model.Phrase]
selectPhrases = Statement.Statement (BS.pack query) Encoders.noParams phraseDecoder True
 where
  query = "SELECT * FROM phrases" -- TODO: make this typed
