module Dal.Queries where

import Dal.Model (PhraseToInsert (..), UserToInsert (..))
import qualified Dal.Model as Model
import Data.Aeson as Aeson
import Data.Aeson.Types (parse)
import qualified Data.ByteString.Char8 as BS
import Data.Text (pack)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Statement as Statement
import Hasql.TH
import Prelude hiding (Success)

-- Encoders

textEncoder :: Encoders.Params Text
textEncoder = Encoders.param (Encoders.nonNullable Encoders.text)

intEncoder :: Encoders.Params Int
intEncoder = contramap fromIntegral (Encoders.param (Encoders.nonNullable Encoders.int4))

jsonEncoder :: Encoders.Params Aeson.Value
jsonEncoder = Encoders.param (Encoders.nonNullable Encoders.json)

errorsEncoder :: Encoders.Params [Model.Error]
errorsEncoder = contramap Aeson.toJSON jsonEncoder

userToInsertEncoder :: Encoders.Params Model.UserToInsert
userToInsertEncoder = contramap username textEncoder <> contramap passwordHash textEncoder

phraseToInsertEncoder :: Encoders.Params Model.PhraseToInsert
phraseToInsertEncoder =
  contramap text textEncoder
    <> contramap errors errorsEncoder
    <> contramap groupId intEncoder
    <> contramap authorId intEncoder

-- Decoders

stringColumn :: Decoders.Row Text
stringColumn = Decoders.column (Decoders.nonNullable Decoders.text)

jsonColumn :: Decoders.Row Aeson.Value
jsonColumn = Decoders.column (Decoders.nonNullable Decoders.json)

idColumn :: Decoders.Row (Model.ID a)
idColumn = fmap (Model.ID . fromIntegral) (Decoders.column (Decoders.nonNullable Decoders.int4))

userRowDecoder :: Decoders.Row Model.User
userRowDecoder = Model.User <$> idColumn <*> stringColumn <*> stringColumn

decodeError :: Aeson.Value -> Aeson.Result [Model.Error]
decodeError = parse parseJSON

phraseRowDecoder :: Decoders.Row Model.Phrase
phraseRowDecoder =
  (,,,,)
    <$> idColumn
    <*> stringColumn
    <*> jsonColumn
    <*> idColumn
    <*> idColumn
    >>= makePhraseRow
 where
  makePhraseRow (i, t, e, g, a) =
    let pe = case decodeError e of
          Success s -> s
          Aeson.Error _ -> []
     in pure $ Model.Phrase i t pe g a

-- Queries
 -- TODO: make queries typed
selectUsers :: Statement.Statement () [Model.User]
selectUsers = Statement.Statement (BS.pack query) Encoders.noParams (Decoders.rowList userRowDecoder) True
 where
  query = "SELECT * FROM users"

insertUser :: Statement.Statement Model.UserToInsert (Maybe Model.User)
insertUser = Statement.Statement (BS.pack query) userToInsertEncoder (Decoders.rowMaybe userRowDecoder) True
 where
  query = "INSERT INTO users (username, password_hash) VALUES ($1, $2) RETURNING *"

findUserByUsername :: Statement.Statement Text (Maybe Model.User)
findUserByUsername = Statement.Statement (BS.pack query) textEncoder (Decoders.rowMaybe userRowDecoder) True
 where
  query = "SELECT * FROM users WHERE username = $1"

insertPhrase :: Statement.Statement Model.PhraseToInsert (Maybe Model.Phrase)
insertPhrase = Statement.Statement (BS.pack query) phraseToInsertEncoder (Decoders.rowMaybe phraseRowDecoder) True
 where
  query = "INSERT INTO phrases (text, errors, group_id, author_id) VALUES ($1, $2, $3, $4) RETURNING *"
